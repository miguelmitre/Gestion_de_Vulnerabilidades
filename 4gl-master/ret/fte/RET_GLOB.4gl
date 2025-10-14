DATABASE safre_af 
GLOBALS
    DEFINE #glo  #g_ret_parametro
	g_ret_parametro       RECORD LIKE ret_parametro.*

    DEFINE #glo  #g_lp_impresoras
	g_lp_impresoras       RECORD LIKE tab_cmd_impresora.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1)
END GLOBALS

FUNCTION provisiona_cuenta_ind(reg_4)
#aci--------------------------------
    DEFINE reg_4 RECORD
        folio                 INTEGER                           ,
        n_seguro              LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        fecha_pago            LIKE dis_cuenta.fecha_pago        ,
        fecha_valor           LIKE dis_cuenta.fecha_valor       ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion  ,
        fecha_archivo         LIKE dis_cuenta.fecha_archivo     ,
        precio_accion         LIKE dis_cuenta.precio_accion     ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos    ,
        consecutivo           INTEGER                           ,
        usuario               CHAR(8)
    END RECORD

    DEFINE #loc #char
        c11_id_aportante      CHAR(11)

    IF reg_4.tipo_movimiento = 560 THEN
        LET c11_id_aportante = "DEV_APL"
    ELSE
        LET c11_id_aportante = "RETIRO"
    END IF

    INSERT INTO safre_af:dis_provision
	VALUES(reg_4.tipo_movimiento   ,
               reg_4.subcuenta         ,
               1                       ,
               reg_4.folio             ,     
               reg_4.consecutivo       ,
               reg_4.n_seguro          ,
               ""                      ,#curp
               ""                      ,#folio_sua
               reg_4.fecha_pago        ,#fecha_pago
               reg_4.fecha_valor       ,#fecha_valor
               reg_4.fecha_conversion  ,#fecha_conversion
               reg_4.monto_en_pesos    ,#monto_en_pesos
               reg_4.monto_en_acciones ,#monto_en_acciones
               reg_4.precio_accion     ,#precio_accion
               0                       ,#dias_cotizados
               ""                      ,#sucursal
               c11_id_aportante        ,#id_aportante
               8                       ,#status
               HOY                     ,#fecha_proceso
               reg_4.usuario           ,#usuario
               reg_4.fecha_archivo     ,#fecha_archivo
               0                        #etiqueta 
              )
END FUNCTION

FUNCTION actualiza_cuenta_ind(reg_4)
#aci--------------------------------
    DEFINE reg_4 RECORD
        folio                 INTEGER                           ,
        n_seguro              LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        siefore               LIKE dis_cuenta.siefore           ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        fecha_pago            LIKE dis_cuenta.fecha_pago        ,
        fecha_valor           LIKE dis_cuenta.fecha_valor       ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion  ,
        precio_accion         LIKE dis_cuenta.precio_accion     ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos    ,
        consecutivo           INTEGER                           ,
        usuario               CHAR(8)
    END RECORD

    DEFINE #loc #char
        c11_id_aportante      CHAR(11)

    IF reg_4.tipo_movimiento = 560 THEN
        LET c11_id_aportante = "DEV_APL"
    ELSE
        LET c11_id_aportante = "RETIRO"
    END IF

    INSERT INTO safre_af:dis_cuenta
	VALUES(reg_4.tipo_movimiento       ,
               reg_4.subcuenta         ,
               reg_4.siefore           ,
               reg_4.folio             ,     
               reg_4.consecutivo       ,
               reg_4.n_seguro          ,
               ""                      ,#curp
               ""                      ,#folio_sua
               reg_4.fecha_pago        ,#fecha_pago
               reg_4.fecha_valor       ,#fecha_valor
               reg_4.fecha_conversion  ,#fecha_conversion
               reg_4.monto_en_pesos    ,#monto_en_pesos
               reg_4.monto_en_acciones ,#monto_en_acciones
               reg_4.precio_accion     ,#precio_accion
               0                       ,#dias_cotizados
               ""                      ,#sucursal
               c11_id_aportante        ,#id_aportante
               8                       ,#status
               HOY                     ,#fecha_proceso
               reg_4.usuario           ,#usuario
               ""                      ,#fecha_archivo
               0                        #etiqueta 
              )
END FUNCTION

FUNCTION habil_siguiente(diaActual,numDiaHabil)
#hs--------------------------------------------
   DEFINE 
       diaTmp	              ,
       diaHabilSig	          ,
       diaActual	          DATE
  
   DEFINE
       cont_1                   ,
       numDiaHabil              ,
       contador	                ,
       diaSemana	        ,
       feriado	                ,
       finSemana	        SMALLINT

   LET cont_1      = 0
   LET diaHabilSig = diaActual

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)  

       IF diaSemana = 0 OR diaSemana = 6 THEN
      	   LET finSemana = 1
       ELSE
           SELECT *
           FROM   tab_feriado 
           WHERE  feria_fecha = diaHabilSig
    	
           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF 
       END IF
		
       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION

####################################################################
#  ESTA FUNCION CALCULA ISR SOLO SE USA PARA SAR ( TIPO RETIRO H )
#  PARA TODOS LOS RETIROS SIN IMPORTAR SEMANAS COTIZADAS LES COBRA 
#  20 % de ISR
####################################################################
FUNCTION calcula_isr_sar(reg_5)
#cisr2----------------------
    DEFINE reg_5 RECORD #loc #reg_5
        n_seguro              LIKE ret_resol_retiro.n_seguro         ,
        consecutivo           LIKE ret_resol_retiro.consecutivo      ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE reg_6 RECORD #loc #reg_6
        semanas_cotiz         LIKE ret_resol_retiro.semanas_cotizadas ,
        salario_minimo        LIKE tabsalario_minimo2.monto_sm     ,
        tot_anos              INTEGER                              ,
        mto_no_grabable       DECIMAL(16,6)                        ,
        mto_grabable          DECIMAL(16,6)                        ,
        retencion             DECIMAL(16,6)                        ,
        mto_neto              DECIMAL(16,6)
    END RECORD


    LET reg_6.mto_grabable = reg_5.monto_en_pesos 

    LET reg_6.retencion      = reg_6.mto_grabable * 0.20
    LET reg_6.mto_neto       = reg_5.monto_en_pesos - reg_6.retencion

    RETURN reg_6.mto_neto, reg_6.retencion
END FUNCTION


FUNCTION calcula_isr(reg_5)
#cisr----------------------
    DEFINE reg_5 RECORD #loc #reg_5
        nss                   LIKE ret_solicitud_tx.nss              ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo      ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE reg_6 RECORD #loc #reg_6
        semanas_cotiz         LIKE ret_solicitud_tx.semanas_cotizadas ,
        salario_minimo        LIKE tabsalario_minimo2.monto_sm     ,
        tot_anos              INTEGER                              ,
        mto_no_grabable       DECIMAL(16,6)                        ,
        mto_grabable          DECIMAL(16,6)                        ,
        retencion             DECIMAL(16,6)                        ,
        mto_neto              DECIMAL(16,6)
    END RECORD

    DEFINE #loc #smallint
        s_resto               ,
        s_estadop             SMALLINT

    DEFINE #loc #integer
        i_delegap             INTEGER

    SELECT B.estado   ,
           B.delega
    INTO   s_estadop ,
           i_delegap
    FROM   afi_mae_afiliado A, afi_domicilio B
    WHERE  A.n_seguro       = reg_5.nss     
    AND    A.n_seguro       = B.nss
    AND    A.n_folio        = B.n_folio
    AND    A.tipo_solicitud = B.tipo_solicitud
    AND    B.marca_envio    = "X"

    SELECT B.monto_sm
    INTO   reg_6.salario_minimo
    FROM   tab_zona_geo A, tabsalario_minimo2 B
    WHERE  A.estad_cod = s_estadop
    AND    A.deleg_cod = i_delegap
    AND    A.zona_cod  = B.zona_cod

    IF reg_6.salario_minimo = 0 OR reg_6.salario_minimo = "" OR
       reg_6.salario_minimo IS NULL
    THEN
        LET reg_6.salario_minimo = 0
        SELECT B.monto_sm
        INTO   reg_6.salario_minimo
        FROM   tabsalario_minimo2 B
        WHERE  B.zona_cod = 'C'
    END IF

    LET reg_6.semanas_cotiz = 0

    SELECT semanas_cotizadas
    INTO   reg_6.semanas_cotiz
    FROM   ret_solicitud_tx
    WHERE  nss         = reg_5.nss
    AND    consecutivo = reg_5.consecutivo

    IF reg_6.semanas_cotiz = 0 OR reg_6.semanas_cotiz = "" THEN
        LET reg_6.mto_neto  = reg_5.monto_en_pesos
        LET reg_6.retencion = 0

        LET reg_6.retencion = reg_5.monto_en_pesos * 0.20
        LET reg_6.mto_neto  = reg_5.monto_en_pesos - reg_6.retencion

        RETURN reg_6.mto_neto, reg_6.retencion
   ELSE
       LET s_resto        = reg_6.semanas_cotiz MOD 52
       LET reg_6.tot_anos = reg_6.semanas_cotiz / 52

       IF s_resto >= 26 THEN
           LET reg_6.tot_anos = reg_6.tot_anos + 1
       END IF

       IF reg_6.tot_anos >= 7 THEN
           LET reg_6.tot_anos = 7
       END IF

       LET reg_6.mto_no_grabable = reg_6.tot_anos * reg_6.salario_minimo * 90

       IF reg_5.monto_en_pesos > reg_6.mto_no_grabable THEN
           LET reg_6.mto_grabable = reg_5.monto_en_pesos - reg_6.mto_no_grabable
           LET reg_6.retencion    = reg_6.mto_grabable * 0.20
           LET reg_6.mto_neto     = reg_5.monto_en_pesos - reg_6.retencion
       ELSE
           LET reg_6.retencion    = 0
           LET reg_6.mto_neto     = reg_5.monto_en_pesos
       END IF

       RETURN reg_6.mto_neto, reg_6.retencion
   END IF
END FUNCTION


FUNCTION calcula_isr2(reg_5)
#cisr2----------------------
    DEFINE reg_5 RECORD #loc #reg_5
        nss                   LIKE ret_solicitud_tx.nss              ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo      ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE reg_6 RECORD #loc #reg_6
        semanas_cotiz         LIKE ret_solicitud_tx.semanas_cotizadas ,
        salario_minimo        LIKE tabsalario_minimo2.monto_sm        ,
        tot_anos              INTEGER                                 ,
        mto_no_grabable       DECIMAL(16,6)                           ,
        mto_grabable          DECIMAL(16,6)                           ,
        retencion             DECIMAL(16,6)                           ,
        mto_neto              DECIMAL(16,6)
    END RECORD

    DEFINE #loc #smallint
        s_descuenta_anos      ,
        s_ano_cotizacion      ,
        s_resto               ,
        s_estadop             SMALLINT

    DEFINE #loc #integer
        i_delegap             INTEGER

    LET s_estadop = 0
    LET i_delegap = 0

    SELECT B.estado   ,
           B.delega
    INTO   s_estadop ,
           i_delegap
    FROM   afi_mae_afiliado A, afi_domicilio B
    WHERE  A.n_seguro       = reg_5.nss     
    AND    A.n_seguro       = B.nss
    AND    A.n_folio        = B.n_folio
    AND    A.tipo_solicitud = B.tipo_solicitud
    AND    B.marca_envio    = "X"

    LET reg_6.salario_minimo = 0

    SELECT B.monto_sm
    INTO   reg_6.salario_minimo
    FROM   tab_zona_geo A, tabsalario_minimo2 B
    WHERE  A.estad_cod = s_estadop
    AND    A.deleg_cod = i_delegap
    AND    A.zona_cod  = B.zona_cod

    IF reg_6.salario_minimo = 0 OR reg_6.salario_minimo = "" THEN
        SELECT B.monto_sm
        INTO   reg_6.salario_minimo
        FROM   tabsalario_minimo2 B
        WHERE  B.zona_cod = 'C'
    END IF

    LET reg_6.semanas_cotiz = 0

    SELECT semanas_cotizadas
    INTO   reg_6.semanas_cotiz
    FROM   ret_solicitud_tx
    WHERE  nss         = reg_5.nss    
    AND    consecutivo = reg_5.consecutivo

    IF reg_6.semanas_cotiz = 0 OR reg_6.semanas_cotiz = "" THEN
        LET reg_6.mto_neto  = reg_5.monto_en_pesos
        LET reg_6.retencion = 0

        LET reg_6.retencion = reg_5.monto_en_pesos * 0.20
        LET reg_6.mto_neto  = reg_5.monto_en_pesos - reg_6.retencion

        RETURN reg_6.mto_neto, reg_6.retencion
   ELSE
        LET s_resto        = reg_6.semanas_cotiz MOD 52
        LET reg_6.tot_anos = reg_6.semanas_cotiz / 52

        IF s_resto >= 26 THEN
            LET reg_6.tot_anos = reg_6.tot_anos + 1
        END IF

        LET s_ano_cotizacion = reg_5.nss[3,4]

        IF reg_6.tot_anos >= 5 THEN
            IF s_ano_cotizacion > 92 THEN
                LET s_descuenta_anos = s_ano_cotizacion - 92
                LET reg_6.tot_anos = 5 - s_descuenta_anos
            ELSE
                LET reg_6.tot_anos = 5
            END IF
        END IF

        LET reg_6.mto_no_grabable = reg_6.tot_anos * reg_6.salario_minimo * 90

        IF reg_5.monto_en_pesos > reg_6.mto_no_grabable THEN
            LET reg_6.mto_grabable = reg_5.monto_en_pesos - reg_6.mto_no_grabable
            LET reg_6.retencion    = reg_6.mto_grabable * 0.20
            LET reg_6.mto_neto     = reg_5.monto_en_pesos - reg_6.retencion
        ELSE
            LET reg_6.retencion    = 0
            LET reg_6.mto_neto     = reg_5.monto_en_pesos
        END IF

        RETURN reg_6.mto_neto, reg_6.retencion
   END IF
END FUNCTION


FUNCTION recupera_saldo (nss, subcuenta, fecha_hasta)
#rs---------------------------------------------------
    DEFINE #loc date
        fecha_hasta	      DATE

    DEFINE #loc char
        nss                   CHAR(11) ,
        subcuenta             CHAR(2)

    DEFINE monto_pesos        DECIMAL(22,6)
    DEFINE monto_acciones     DECIMAL(22,6)
    DEFINE monto_actual       DECIMAL(22,6)

    DEFINE fecha_tope 	      DATE
    DEFINE anio_4  	      CHAR(4)
    DEFINE valor_accion       DECIMAL(11,6)

   --WHENEVER ERROR CONTINUE  

   LET monto_pesos    = 0
   LET monto_acciones = 0
   LET monto_actual   = 0
   LET fecha_tope = fecha_hasta + 1
   LET anio_4 = YEAR(fecha_hasta)


   IF  anio_4 = "1997"             THEN
       SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
       INTO   monto_pesos,         monto_acciones
       FROM   dis_cuenta97 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_conversion < fecha_tope
   ELSE  
       IF  anio_4 = "1998"             THEN
           SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
           INTO   monto_pesos,         monto_acciones
           FROM   dis_cuenta98 a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_conversion < fecha_tope
       ELSE  
           IF  anio_4 = "1999"             THEN
               SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
               INTO   monto_pesos,         monto_acciones
               FROM   dis_cuenta99 a
               WHERE  a.nss              = nss
               AND    a.subcuenta        = subcuenta
               AND    a.fecha_conversion < fecha_tope
           ELSE  
               IF  anio_4 = "2000"             THEN
                   SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
                   INTO   monto_pesos, monto_acciones
                   FROM   dis_cuenta00 a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_conversion <= fecha_hasta
               ELSE   
                   IF  anio_4 = "2001"             THEN
                       SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
                       INTO   monto_pesos, monto_acciones
                       FROM   dis_cuenta01 a
                       WHERE  a.nss              = nss
                       AND    a.subcuenta        = subcuenta
                       AND    a.fecha_conversion <= fecha_hasta
                   ELSE   
                       SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
                       INTO   monto_pesos,         monto_acciones
                       FROM   dis_cuenta a
                       WHERE  a.nss              = nss
                       AND    a.subcuenta        = subcuenta
                       AND    a.fecha_conversion < fecha_tope
                   END IF
               END IF
           END IF
       END IF
   END IF

   LET valor_accion = 1
   IF monto_acciones IS NULL OR monto_acciones = " " THEN
       LET monto_acciones = 0
   END IF
   IF monto_pesos IS NULL OR monto_pesos = " " THEN
       LET monto_pesos = 0
   END IF
   LET monto_actual = monto_acciones * valor_accion

   RETURN monto_pesos, monto_acciones, monto_actual
END FUNCTION

FUNCTION recupera_saldo2(nss, subcuenta, fecha_hasta)
#rs---------------------------------------------------
    DEFINE #loc date
        fecha_hasta	      DATE

    DEFINE #loc char
        nss                   CHAR(11) ,
        subcuenta             CHAR(2)

    DEFINE monto_pesos        DECIMAL(22,6)
    DEFINE monto_acciones     DECIMAL(22,6)
    DEFINE monto_accion2      DECIMAL(22,6)
    DEFINE monto_accion3      DECIMAL(22,6)
    DEFINE monto_accion4      DECIMAL(22,6)
    DEFINE monto_accion5      DECIMAL(22,6)
    DEFINE monto_accion6      DECIMAL(22,6)

    DEFINE fecha_tope 	      DATE
    DEFINE anio_4  	      CHAR(4)
    DEFINE valor_accion       DECIMAL(11,6)

   --WHENEVER ERROR CONTINUE  

   LET monto_pesos    = 0
   LET monto_acciones = 0
   LET monto_accion2  = 0
   LET monto_accion3  = 0
   LET monto_accion4  = 0
   LET monto_accion5  = 0
   LET monto_accion6  = 0

--   LET fecha_tope = fecha_hasta + 1
   LET anio_4 = YEAR(fecha_hasta)

   IF  anio_4 = "1997"             THEN
       SELECT SUM(a.monto_en_acciones)
       INTO   monto_acciones
       FROM   dis_cuenta97 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_valor < fecha_hasta        

       SELECT SUM(a.monto_en_acciones)
       INTO   monto_accion2
       FROM   dis_cuenta98 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_valor < fecha_hasta        

       SELECT SUM(a.monto_en_acciones)
       INTO   monto_accion3
       FROM   dis_cuenta99 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_valor < fecha_hasta        

       SELECT SUM(a.monto_en_acciones)
       INTO   monto_accion4
       FROM   dis_cuenta00 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_valor < fecha_hasta        

       SELECT SUM(a.monto_en_acciones)
       INTO   monto_accion5
       FROM   dis_cuenta01 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_valor < fecha_hasta        

       SELECT SUM(a.monto_en_acciones)
       INTO   monto_accion6
       FROM   dis_cuenta a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_valor < fecha_hasta        
   ELSE  
       IF  anio_4 = "1998"             THEN
           SELECT SUM(a.monto_en_acciones)
           INTO   monto_acciones
           FROM   dis_cuenta98 a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_valor < fecha_hasta        

           SELECT SUM(a.monto_en_acciones)
           INTO   monto_accion2
           FROM   dis_cuenta99 a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_valor < fecha_hasta        

           SELECT SUM(a.monto_en_acciones)
           INTO   monto_accion3
           FROM   dis_cuenta00 a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_valor < fecha_hasta        

           SELECT SUM(a.monto_en_acciones)
           INTO   monto_accion4
           FROM   dis_cuenta01 a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_valor < fecha_hasta        

           SELECT SUM(a.monto_en_acciones)
           INTO   monto_accion5
           FROM   dis_cuenta a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_valor < fecha_hasta        
       ELSE  
           IF  anio_4 = "1999"             THEN
               SELECT SUM(a.monto_en_acciones)
               INTO   monto_acciones
               FROM   dis_cuenta99 a
               WHERE  a.nss              = nss
               AND    a.subcuenta        = subcuenta
               AND    a.fecha_valor < fecha_hasta        

               SELECT SUM(a.monto_en_acciones)
               INTO   monto_accion2
               FROM   dis_cuenta00 a
               WHERE  a.nss              = nss
               AND    a.subcuenta        = subcuenta
               AND    a.fecha_valor < fecha_hasta        

               SELECT SUM(a.monto_en_acciones)
               INTO   monto_accion3
               FROM   dis_cuenta01 a
               WHERE  a.nss              = nss
               AND    a.subcuenta        = subcuenta
               AND    a.fecha_valor < fecha_hasta        

               SELECT SUM(a.monto_en_acciones)
               INTO   monto_accion4
               FROM   dis_cuenta a
               WHERE  a.nss              = nss
               AND    a.subcuenta        = subcuenta
               AND    a.fecha_valor < fecha_hasta        
           ELSE  
               IF  anio_4 = "2000"             THEN
                   SELECT SUM(a.monto_en_acciones)
                   INTO   monto_acciones
                   FROM   dis_cuenta00 a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_valor < fecha_hasta        

                   SELECT SUM(a.monto_en_acciones)
                   INTO   monto_accion2
                   FROM   dis_cuenta01 a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_valor < fecha_hasta        

                   SELECT SUM(a.monto_en_acciones)
                   INTO   monto_accion3
                   FROM   dis_cuenta a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_valor < fecha_hasta        
               ELSE   
                   IF  anio_4 = "2001"             THEN
                       SELECT SUM(a.monto_en_acciones)
                       INTO   monto_acciones
                       FROM   dis_cuenta01 a
                       WHERE  a.nss              = nss
                       AND    a.subcuenta        = subcuenta
                       AND    a.fecha_valor < fecha_hasta        

                       SELECT SUM(a.monto_en_acciones)
                       INTO   monto_accion2
                       FROM   dis_cuenta a
                       WHERE  a.nss              = nss
                       AND    a.subcuenta        = subcuenta
                       AND    a.fecha_valor < fecha_hasta        
		   ELSE
                       SELECT SUM(a.monto_en_acciones)
                       INTO   monto_acciones
                       FROM   dis_cuenta a
                       WHERE  a.nss              = nss
                       AND    a.subcuenta        = subcuenta
                       AND    a.fecha_valor < fecha_hasta        
                   END IF
               END IF
           END IF
       END IF
   END IF

   LET valor_accion = 1
   IF monto_acciones IS NULL OR monto_acciones = " " THEN
       LET monto_acciones = 0
   END IF
 
   IF monto_accion2 IS NULL OR monto_accion2 = " " THEN
       LET monto_accion2 = 0
   END IF

   IF monto_accion3 IS NULL OR monto_accion3 = " " THEN
       LET monto_accion3 = 0
   END IF

   IF monto_accion4 IS NULL OR monto_accion4 = " " THEN
       LET monto_accion4 = 0
   END IF

   IF monto_accion5 IS NULL OR monto_accion5 = " " THEN
       LET monto_accion5 = 0
   END IF

   IF monto_accion6 IS NULL OR monto_accion6 = " " THEN
       LET monto_accion6 = 0
   END IF
 
--   LET monto_actual = monto_acciones * valor_accion
   LET monto_acciones = monto_acciones + monto_accion2 + monto_accion3 +
                        monto_accion4  + monto_accion5 + monto_accion6
   RETURN monto_acciones
END FUNCTION

FUNCTION num_dias_mes(mes)
#ndm----------------------
    DEFINE 
        mes       CHAR(02) ,
        num_dias  CHAR(02)

    IF  LENGTH(mes) = 1 THEN
	LET mes = '0',mes
    END IF

    CASE mes
        WHEN "01"  
            LET num_dias = 31
        WHEN "02"  
            LET num_dias = 28
        WHEN "03"  
            LET num_dias = 31
        WHEN "04"  
            LET num_dias = 30
        WHEN "05"  
            LET num_dias = 31
        WHEN "06"  
            LET num_dias = 30
        WHEN "07"  
            LET num_dias = 31
        WHEN "08"  
            LET num_dias = 31
        WHEN "09"  
            LET num_dias = 30
        WHEN "10" 
            LET num_dias = 31
        WHEN "11" 
            LET num_dias = 30
        WHEN "12" 
            LET num_dias = 31
    END CASE
RETURN num_dias
END FUNCTION

Function inhabilita(reg_1)
#fh-----------------------
    DEFINE
        precio_accion         LIKE glo_valor_accion.precio_del_dia  

    DEFINE reg_1 RECORD #glo #reg_1
        folio                 INTEGER  ,
        n_seguro              CHAR(11) ,
        tipo_retiro           CHAR(01) ,
        consecutivo           INTEGER
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        tipo_movimiento       INTEGER       ,
        acc_subcta1           DECIMAL(16,6) ,
        acc_subcta2           DECIMAL(16,6) ,
        acc_subcta3           DECIMAL(16,6) ,
        acc_subcta5           DECIMAL(16,6) ,
        acc_subcta6           DECIMAL(16,6) ,
        acc_subcta7           DECIMAL(16,6) ,
        acc_subcta9           DECIMAL(16,6) ,
        acc_subcta10          DECIMAL(16,6) ,
        acciones              DECIMAL(16,6) ,
        pesos_subcta4         DECIMAL(16,6) ,
        pesos_subcta8         DECIMAL(16,6) ,
        pesos_viv             DECIMAL(16,6)
    END RECORD

    DEFINE #char
        HORA                  CHAR(0008)
    
    LET HOY  = TODAY
    LET HORA = TIME

    SELECT A.precio_del_dia
    INTO   precio_accion
    FROM   glo_valor_accion A
    WHERE  A.fecha_valuacion = HOY

    ---RETIRO 97----------------------------------------------------------
    LET reg_2.acc_subcta1  = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta1
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta       = 1
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta1 < 0 THEN
       LET reg_2.acc_subcta1  = 0
    END IF
    ----------------------------------------------------------------------

    ---CESANTIA Y VEJEZ---------------------------------------------------
    LET reg_2.acc_subcta2  = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta2
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta       = 2
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta2 < 0 THEN
       LET reg_2.acc_subcta2  = 0
    END IF
    ----------------------------------------------------------------------

    ---APORTACIONES VOLUNTARIAS VENTANILLA--------------------------------
    LET reg_2.acc_subcta3  = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta3
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta       = 3
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta3 < 0 THEN
       LET reg_2.acc_subcta3  = 0
    END IF
    ----------------------------------------------------------------------

    ---CUOTA SOCIAL-------------------------------------------------------
    LET reg_2.acc_subcta5  = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta5
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta       = 5
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta5 < 0 THEN
       LET reg_2.acc_subcta5  = 0
    END IF
    ----------------------------------------------------------------------

    ---APORTACIONES ESTATAL-----------------------------------------------
    LET reg_2.acc_subcta6  = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta6
    FROM   dis_cuenta
    WHERE  nss        = reg_1.n_seguro
    AND    subcuenta  = 6
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta6 < 0 THEN
       LET reg_2.acc_subcta6  = 0
    END IF
    ----------------------------------------------------------------------

    ---SAR 92-------------------------------------------------------------
    LET reg_2.acc_subcta7  = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta7
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta       = 7
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta7 < 0 THEN
       LET reg_2.acc_subcta7  = 0
    END IF
    ----------------------------------------------------------------------

    ---APORTACION ESPECIAL------------------------------------------------
    LET reg_2.acc_subcta9  = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta9
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta       = 9
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta9 < 0 THEN
       LET reg_2.acc_subcta9  = 0
    END IF
    ----------------------------------------------------------------------

    ---VOLUNTARIAS PATRONALES---------------------------------------------
    LET reg_2.acc_subcta10 = 0

    SELECT NVL(SUM(monto_en_acciones),0)
    INTO   reg_2.acc_subcta10
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta       = 10
    AND    tipo_movimiento > 0

    IF reg_2.acc_subcta10 < 0 THEN
       LET reg_2.acc_subcta10  = 0
    END IF
    ----------------------------------------------------------------------

    LET reg_2.acciones     = 0
    LET reg_2.acciones  =  reg_2.acc_subcta1 +
                           reg_2.acc_subcta2 +
                           reg_2.acc_subcta3 +
                           reg_2.acc_subcta5 +
                           reg_2.acc_subcta6 +
                           reg_2.acc_subcta7 +
                           reg_2.acc_subcta9 +
                           reg_2.acc_subcta10 

    ---VIVIENDA------------------------------------------------------------
    LET reg_2.pesos_subcta4 = 0

    SELECT NVL(SUM(monto_en_pesos),0)
    INTO   reg_2.pesos_subcta4
    FROM   dis_cuenta
    WHERE  nss             =  reg_1.n_seguro
    AND    subcuenta       =  4
    AND    tipo_movimiento <> 888

    IF reg_2.pesos_subcta4 < 0 THEN
       LET reg_2.pesos_subcta4  = 0
    END IF
    ----------------------------------------------------------------------

    ---VIVIENDA ANTERIOR---------------------------------------------------
    LET reg_2.pesos_subcta8 = 0

    SELECT NVL(SUM(monto_en_pesos),0)
    INTO   reg_2.pesos_subcta8
    FROM   dis_cuenta
    WHERE  nss             =  reg_1.n_seguro
    AND    subcuenta       =  8
    AND    tipo_movimiento <> 888

    IF reg_2.pesos_subcta8 < 0 THEN
       LET reg_2.pesos_subcta8  = 0
    END IF
    ----------------------------------------------------------------------

    LET reg_2.pesos_viv = 0
    LET reg_2.pesos_viv = reg_2.pesos_subcta4 +
                          reg_2.pesos_subcta8

    IF reg_2.acciones = 0 AND reg_2.pesos_viv = 0 THEN
       # INSERT INTO safre_tmp:borrar values(reg_1.n_seguro  ,
       #                       	             reg_2.pesos_viv ,
       #	 			     reg_2.acciones
       #			             )
       CASE 
           WHEN  reg_1.tipo_retiro = 'D' 
               SELECT movimiento  
               INTO   reg_2.tipo_movimiento
               FROM   tab_retiro
               WHERE  tipo_retiro = 'D'
           WHEN  reg_1.tipo_retiro = 'H' 
               SELECT movimiento  
               INTO   reg_2.tipo_movimiento
               FROM   tab_retiro
               WHERE  tipo_retiro = 'H'
        END CASE


        --Si la cuenta ya fue inhabilitada no debe de inhabilitar-----------
        SELECT "X" 
	FROM   cta_act_marca 
        WHERE  nss       = reg_1.n_seguro
        AND    marca_cod = 5                      
	GROUP BY 1
               
	IF STATUS = NOTFOUND THEN
            SELECT "X"
            FROM   cta_ctr_cuenta
            WHERE  nss = reg_1.n_seguro

            IF STATUS <> NOTFOUND THEN      
		SET LOCK MODE TO WAIT 10
               -- UPDATE cta_ctr_cuenta
               -- SET    estado_cuenta     = 5                     ,
	       --        fecha_edo_cuenta  = HOY                   ,     
               --        marca_cod         = reg_2.tipo_movimiento ,
               --        activo_marca      = 1                     ,
               --        fecha_act_marca   = HOY                   ,     
	       --        tipo_informe      = 5                     ,
	       --        fecha_informe     = HOY
               --  WHERE  nss = reg_1.n_seguro
                
               -- INSERT INTO cta_act_marca 
	       -- VALUES(reg_1.n_seguro        ,#nss         char(11)
               --        5                     ,#marca_cod   smallint
               --        HOY                   ,#fecha_ini   date
               --        HORA                  ,#hora_ini    datetime
               --        0                     ,#estado      smallint
               --        reg_2.tipo_movimiento ,#marca_causa smallint
               --        HOY                   ,#fecha_causa date
               --        reg_1.consecutivo     ,#correlativo integer
               --        USER                   #usuario     char(8)
               --       )

              --  INSERT INTO cta_his_marca 
	      --  VALUES(reg_1.n_seguro        ,#nss         char(11)
              --         5                     ,#marca_cod   smallint
              --         HOY                   ,#fecha_ini   date
	      --         NULL                  ,#fecha_fin   date
              --         HORA                  ,#hora_ini    datetime
              --         0                     ,#estado      smallint
	      --         0                     ,#rechazo_cod smallint
              --         reg_2.tipo_movimiento ,#marca_causa smallint
              --         HOY                   ,#fecha_causa date
              --         reg_1.consecutivo     ,#correlativo integer
              --         USER                   #usuario     char(8)
              --        )

                INSERT INTO ret_his_inhabilita
		VALUES (reg_1.folio           ,
                        reg_1.n_seguro        ,
                        reg_2.tipo_movimiento ,
                        reg_2.acciones        ,
                        reg_2.pesos_viv       ,
                        HOY                   ,
                        HOY                   ,
			reg_1.consecutivo     ,
			1                     -- inhabilito
		       )
		SET LOCK MODE TO NOT WAIT 
            ELSE
                INSERT INTO ret_his_inhabilita
		VALUES (reg_1.folio           ,
                        reg_1.n_seguro        ,
                        reg_2.tipo_movimiento ,
                        reg_2.acciones        ,
                        reg_2.pesos_viv       ,
                        HOY                   ,
                        HOY                   ,
			reg_1.consecutivo     ,
			3                     -- No existe registro en
		       )                      -- cta_ctr_cuenta
	    END IF
	ELSE
            INSERT INTO ret_his_inhabilita
	    VALUES (reg_1.folio           ,
                    reg_1.n_seguro        ,
                    reg_2.tipo_movimiento ,
                    reg_2.acciones        ,
                    reg_2.pesos_viv       ,
                    HOY                   ,
                    HOY                   ,
		    reg_1.consecutivo     ,
		    2                     -- No inhabilito, ya se encontraba
		   )                      -- inhabilitado
        END IF
        --------------------------------------------------------------------
    END IF
END FUNCTION

FUNCTION imprime_reporte_inhabilita(folio_op,tipo_retiro)
#iri----------------------------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        folio                 INTEGER       ,
        n_seguro              CHAR(11)      ,
	nombre                CHAR(50)      ,
        tipo_movimiento       INTEGER       ,
        acciones              DECIMAL(16,6) ,
        pesos_viv             DECIMAL(16,6) ,
        fech_val_acc          DATE          ,   
        consecutivo           INTEGER 
    END RECORD

    DEFINE 
	precio_accion         LIKE glo_valor_accion.precio_del_dia   

    DEFINE #integer
	folio_op                    ,
        reg_procesados        INTEGER

    DEFINE #smallint
	tipo_retiro  SMALLINT

    DEFINE #char(40) 
	vnombres              ,
	vpaterno              , 
	vmaterno     CHAR(40) ,
	lp                    ,
        G_LISTA      CHAR(100),
        ch           CHAR(110),
        afore_local  CHAR(03) ,
        HORA         CHAR(05)

    SELECT *
      INTO g_ret_parametro.*
      FROM ret_parametro

    SELECT codigo_afore
      INTO afore_local
      FROM tab_afore_local

    SELECT *
      INTO g_lp_impresoras.*
      FROM tab_cmd_impresora
     WHERE codigo_afore = afore_local 

    SELECT A.precio_del_dia
      INTO precio_accion
      FROM glo_valor_accion A
     WHERE A.fecha_valuacion = HOY

    LET HORA = TIME

    LET G_LISTA = g_ret_parametro.ruta_spool CLIPPED,"/",
		 HOY USING "DDMMYYYY",".",tipo_retiro USING "&&&",".",HORA

    LET lp = g_lp_impresoras.comando_impresion,G_LISTA

    DECLARE cur_rep CURSOR FOR 
    SELECT folio             , 
           n_seguro          ,
           tipo_movimiento   , 
           acciones          ,
           pesos_viv         ,
           fech_val_acc      , 
           consecutivo         
    FROM   ret_his_inhabilita   
    WHERE  @folio = folio_op   
    AND    @status = 1

    START REPORT listado_100 TO G_LISTA

    LET reg_procesados = 0
    FOREACH cur_rep INTO reg_3.folio, 
                       reg_3.n_seguro,
                       reg_3.tipo_movimiento,
                       reg_3.acciones,
                       reg_3.pesos_viv,
                       reg_3.fech_val_acc,      
                       reg_3.consecutivo       

        LET reg_procesados = reg_procesados + 1      
        LET reg_3.acciones = reg_3.acciones * precio_accion

        SELECT nombres,paterno,materno 
	INTO   vnombres , vpaterno , vmaterno
        FROM   afi_mae_afiliado
       --FROM   ret_resol_retiro
	WHERE  n_seguro = reg_3.n_seguro
       -- AND    consecutivo = reg_3.consecutivo 

	LET reg_3.nombre = vpaterno CLIPPED ," ",vmaterno CLIPPED," ",vnombres CLIPPED

        OUTPUT TO REPORT listado_100(reg_3.folio           ,
                                     reg_3.n_seguro        ,
                                     reg_3.nombre          ,
                                     reg_3.tipo_movimiento ,
                                     reg_3.acciones        ,
                                     reg_3.pesos_viv       ,
                                     reg_3.fech_val_acc    , 
                                     reg_3.consecutivo      
                                     )
        INITIALIZE reg_3.* TO NULL
    END FOREACH
    FINISH REPORT listado_100
    LET ch = "chmod 777 ",G_LISTA
    RUN ch
    RUN lp
END FUNCTION 

FUNCTION imprime_reporte_inhabilita2(folio_op,tipo_retiro)
#iri----------------------------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        folio                 INTEGER       ,
        n_seguro              CHAR(11)      ,
	nombre                CHAR(50)      ,
        tipo_movimiento       INTEGER       ,
        acciones              DECIMAL(16,6) ,
        pesos_viv             DECIMAL(16,6) ,
        fech_val_acc          DATE          ,   
        consecutivo           INTEGER 
    END RECORD

    DEFINE 
	precio_accion         LIKE glo_valor_accion.precio_del_dia   

    DEFINE #integer
	folio_op                    ,
        reg_procesados        INTEGER

    DEFINE #smallint
	tipo_retiro  SMALLINT

    DEFINE #char(40) 
	vnombres              ,
	vpaterno              , 
	vmaterno     CHAR(40) ,
	lp                    ,
        G_LISTA      CHAR(100),
        ch           CHAR(110),
        afore_local  CHAR(03) ,
        HORA         CHAR(05)

    SELECT *
    INTO   g_ret_parametro.*
    FROM   ret_parametro

    SELECT codigo_afore
    INTO   afore_local
    FROM   tab_afore_local

    SELECT *
    INTO   g_lp_impresoras.*
    FROM   tab_cmd_impresora
    WHERE  codigo_afore = afore_local 

    SELECT A.precio_del_dia
    INTO   precio_accion
    FROM   glo_valor_accion A
    WHERE  A.fecha_valuacion = HOY

    LET HORA = TIME

    LET G_LISTA = g_ret_parametro.ruta_spool CLIPPED,"/",
		 HOY USING "DDMMYYYY",".",tipo_retiro USING "&&&",".",HORA

    LET lp = g_lp_impresoras.comando_impresion CLIPPED," ",G_LISTA

    DECLARE cur_rep2 CURSOR FOR 
    SELECT folio           , 
           n_seguro        ,
           tipo_movimiento , 
           acciones        ,
           pesos_viv       ,
           fech_val_acc    , 
           consecutivo         
    FROM   ret_his_inhabilita   
    WHERE  @folio  = folio_op   
    AND    @status = 1

    START REPORT listado_100 TO G_LISTA

    LET reg_procesados = 0
    FOREACH cur_rep2 INTO reg_3.folio, 
                       reg_3.n_seguro,
                       reg_3.tipo_movimiento,
                       reg_3.acciones,
                       reg_3.pesos_viv,
                       reg_3.fech_val_acc,      
                       reg_3.consecutivo       

        LET reg_procesados = reg_procesados + 1      
        LET reg_3.acciones = reg_3.acciones * precio_accion

        SELECT nombres,paterno,materno 
	INTO   vnombres , vpaterno , vmaterno
        FROM   afi_mae_afiliado
       --FROM   ret_resol_retiro
	WHERE  n_seguro = reg_3.n_seguro
       -- AND    consecutivo = reg_3.consecutivo 

	LET reg_3.nombre = vpaterno CLIPPED ," ",vmaterno CLIPPED," ",vnombres CLIPPED

        OUTPUT TO REPORT listado_100(reg_3.folio           ,
                                     reg_3.n_seguro        ,
                                     reg_3.nombre          ,
                                     reg_3.tipo_movimiento ,
                                     reg_3.acciones        ,
                                     reg_3.pesos_viv       ,
                                     reg_3.fech_val_acc    , 
                                     reg_3.consecutivo      
                                     )
        INITIALIZE reg_3.* TO NULL
    END FOREACH
    FINISH REPORT listado_100
    LET ch = "chmod 777 ",G_LISTA
    RUN ch

    RETURN lp, reg_procesados
END FUNCTION 

REPORT listado_100(reg_3)
#l1--------------------
    DEFINE reg_3 RECORD #loc #reg_3
        folio                 INTEGER       ,
        n_seguro              CHAR(11)      ,
	nombre                CHAR(50)      ,
        tipo_movimiento       INTEGER       ,
        acciones              DECIMAL(8,2) ,
        pesos_viv             DECIMAL(8,2) ,
        fech_val_acc          DATE          ,   
        consecutivo           INTEGER 
    END RECORD

    DEFINE L1                 CHAR(01)
    DEFINE L5                 CHAR(05)
    DEFINE L10                CHAR(10)
    DEFINE L11                CHAR(11)
    DEFINE L18                CHAR(18)                      

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH  90

    FORMAT

    PAGE HEADER
 
    LET L1  = "\304"
    LET L5  = "\304\304\304\304\304"
    LET L10 = "\304\304\304\304\304\304\304\304\304\304"
    LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"
    LET L18 =
    "\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304"

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

        PRINT COLUMN 42,"S U B D I R E C C I O N  D E  B E N E F I C I O S"

        SKIP 1 LINES

        PRINT COLUMN 42,"              CUENTAS INHABILITADAS              "

        SKIP 2 LINES

        PRINT COLUMN 105,"PROG.    : RETC023"
        PRINT
        PRINT COLUMN 105,"PAGINA   :    ",PAGENO USING "####"
        PRINT
        PRINT COLUMN 105,"FECHA : ", TODAY USING "DD/MM/YYYY"
        PRINT
              COLUMN 001,"\332",L10,L1,"\302",L10,L10,L10,L10,L5,L1,L1,L1,L1,
		    "\302",L10,"\302",L1,L1,L1,L1,L10,L10,L10,L10,L1,L1,L1,
                    "\277"
        PRINT
              COLUMN 001,"|"                            ,
              COLUMN 013,"|"                            ,
              COLUMN 063,"|"                            ,
              COLUMN 074,"|"                            ,
              COLUMN 082,"  T O T A L  E N  P E S O S " ,
              COLUMN 122,"|"                     
        PRINT
              COLUMN 001,"|"                            ,
              COLUMN 013,"|"                            ,
              COLUMN 063,"|"                            ,
              COLUMN 074,"|"                            ,
              COLUMN 122,"|"                     
        PRINT
              COLUMN 001,"|"                     ,
              COLUMN 006,"NSS"                   ,
              COLUMN 013,"|"                     ,
              COLUMN 027,"NOMBRE DEL TRABAJADOR" ,
              COLUMN 063,"|"                     ,
              COLUMN 064,"TIPO MVTO."            ,
              COLUMN 074,"|"                     ,
              COLUMN 085,"RCV SAR VOL "          ,
              COLUMN 098,"  VIVIENDA  "          ,
              COLUMN 122,"|"                     

        PRINT
              COLUMN 001,"\300",L10,L1,"\301",L10,L10,L10,L10,L5,L1,L1,L1,L1,
                         "\301",L10,"\301",L1,L1,L1,L1,L10,L10,L10,L10,L1,L1,L1,
                         "\331"

    BEFORE GROUP OF reg_3.folio
        PRINT
        PRINT
        PRINT
              COLUMN 002,"FOLIO  :",reg_3.folio
        SKIP 1 LINES

    ON EVERY ROW
        PRINT
        PRINT
              COLUMN 002,reg_3.n_seguro                         ,
              COLUMN 014,reg_3.nombre                           ,
              COLUMN 064,reg_3.tipo_movimiento USING "###"      ,
              COLUMN 080,reg_3.acciones        ,        
              COLUMN 098,reg_3.pesos_viv       
END REPORT

FUNCTION determina_fech_compara(HOY,dias_correr)
#dfc-------------------------------------------
    DEFINE #date
	 HOY                ,
	 d_fecha_fin    DATE

    DEFINE #char 
	 c10_fecha_fin  CHAR(10),
	 ult_dia_mes    CHAR(02)

    DEFINE #integer
	 dias_correr    INTEGER

         IF MONTH(HOY)  =  01   THEN
	     IF DAY(HOY) = 01 THEN
		 LET c10_fecha_fin = '12/31/',YEAR(HOY) - 1
		 LET d_fecha_fin   = c10_fecha_fin
		 CALL habil_anterior(d_fecha_fin,1) RETURNING d_fecha_fin
             ELSE
		 CALL habil_anterior(HOY,dias_correr) 
		      RETURNING d_fecha_fin
             END IF
         ELSE
	     IF DAY(HOY) = 01 THEN
		 CALL num_dias_mes(MONTH(HOY)-1) RETURNING ult_dia_mes
		 LET c10_fecha_fin = MDY(MONTH(HOY)-1,ult_dia_mes,YEAR(HOY))
		 LET d_fecha_fin   = c10_fecha_fin
		 CALL habil_anterior(d_fecha_fin,1) RETURNING d_fecha_fin
             ELSE
		 CALL habil_anterior(HOY,dias_correr)
		      RETURNING d_fecha_fin
             END IF
         END IF

    RETURN d_fecha_fin
END FUNCTION

FUNCTION habil_anterior(diaActual,num_dia)
#ha---------------------------------------
    DEFINE #smallint
	cont_1                 ,
	feriado	               ,
	finSemana	       ,
	diaSemana	       ,
   	contador	       ,
        num_dia                SMALLINT

    DEFINE #date
	diaActual	       ,
        diaHabilAnt	       ,
	diaTmp	               DATE

    LET cont_1      = 0
   #LET diaHabilAnt = diaActual - 1 UNITS DAY
    LET diaHabilAnt = diaActual 

    WHILE TRUE
        LET feriado   = 0
   	LET finSemana = 0
   	LET diaSemana = WEEKDAY(diaHabilAnt)  

   	IF diaSemana = 0 OR diaSemana = 6 THEN
      	    LET finSemana = 1
  	ELSE     
   	    SELECT *
   	    FROM   tab_feriado
   	    WHERE  feria_fecha = diaHabilAnt
    	
   	    IF STATUS <> NOTFOUND THEN
       	        LET feriado = 1
   	    END IF 
   	END IF
		
   	IF feriado = 1 OR finSemana = 1 THEN
       	    LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
        ELSE
	    LET cont_1      = cont_1 + 1

	    IF cont_1 = num_dia THEN
	        EXIT WHILE
            ELSE
       	        LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
	    END IF
   	END IF
    END WHILE

    RETURN diaHabilAnt
END FUNCTION

FUNCTION matriz_de_derecho(reg_2)
#rmd------------------------
    DEFINE   reg_2  RECORD  #glo #reg_2 
        regimen                      ,
        tipo_seguro                  ,
        tipo_pension         CHAR(02),
        tipo_prestacion      SMALLINT
    END RECORD

    DEFINE #glo #char
        enter                CHAR(01)

    DEFINE #glo #smallint
        x_grupo              SMALLINT

    SELECT grupo
    INTO   x_grupo
    FROM   ret_matriz_derecho
    WHERE  regimen         = reg_2.regimen
    AND    tipo_seguro     = reg_2.tipo_seguro
    AND    tipo_pension    = reg_2.tipo_pension
    AND    tipo_prestacion = reg_2.tipo_prestacion
    GROUP BY 1

    IF SQLCA.SQLCODE = NOTFOUND  THEN
        PROMPT " NO EXISTE RESGISTRO EN LA RET MATRIZ DERECHO " FOR CHAR enter
        EXIT PROGRAM
    END IF
       
    RETURN x_grupo
END FUNCTION

FUNCTION Despliega_localidades()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
	DISPLAY "                   L O C A L I D A D E S                 " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

     
	WHILE TRUE
	      LET x_x = " SELECT local_cod,local_desc FROM safre_af:tablocal ",
	                " WHERE local_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg1 FROM x_x
	      DECLARE cur_g1 CURSOR FOR curg1
	      LET pos = 1
	      FOREACH cur_g1 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO LOCALIDADES... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_estados()
	DEFINE aux_val		SMALLINT
	DEFINE x_x CHAR(100)
	DEFINE x_buscar CHAR(30)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
	DISPLAY "                ENTIDADES   FEDERATIVAS               " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	      
	WHILE TRUE
	      LET x_x = " SELECT estad_cod,estad_desc FROM safre_af:tab_estado ",
	                " WHERE estad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg2 FROM x_x
	      DECLARE cur_g2 CURSOR FOR curg2
	      LET pos = 1
	      FOREACH cur_g2 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO ESTADOS..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_delegaciones()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
	DISPLAY "                 D E L E G A C I O N E S                  " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT deleg_cod,deleg_desc FROM safre_af:tab_delegacion ",
	                " WHERE deleg_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg5 FROM x_x
	      DECLARE cur_g5 CURSOR FOR curg5
	      LET pos = 1
	      FOREACH cur_g5 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO DELEGACIONES..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_ciudades()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
	DISPLAY "                     C I U D A D E S                      " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      BEFORE   FIELD x_buscar
		   LET      x_buscar   =  "*"
		   DISPLAY  BY  NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT ciudad_cod,ciudad_desc FROM safre_af:tab_ciudad ",
	                " WHERE ciudad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg6 FROM x_x
	      DECLARE cur_g6 CURSOR FOR curg6
	      LET pos = 1
	      FOREACH cur_g6 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO CIUDADES..... VACIO"
              END IF 	
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION


FUNCTION Despliega_parentescos()
    DEFINE aux_val		SMALLINT
    DEFINE l_reg ARRAY[1000] OF RECORD
              codigo		INTEGER,
              descripcion	CHAR(50)
           END RECORD
    DEFINE x_x		        char(100),
           x_buscar		char(30)
    DEFINE pos	   	        SMALLINT

    OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
    DISPLAY "                 P A R E N T E S C O S                   " AT 2,1 ATTRIBUTE(REVERSE)

      LET x_x = " SELECT paren_cod,paren_desc FROM safre_af:tab_parentesco ",
                " WHERE paren_desc MATCHES '*' ",
                " ORDER BY 1 " CLIPPED
      PREPARE curg7 FROM x_x
      DECLARE cur_g7 CURSOR FOR curg7
      LET pos = 1
      FOREACH cur_g7 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
	       ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
	       EXIT FOREACH
	    END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO PARENTESCO..... VACIO"
      END IF
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg TO scr_1.*

         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY

      END DISPLAY

    CLOSE WINDOW vent_1
    RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION


FUNCTION Despliega_pais()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		char(03),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
	DISPLAY "                    P A I S E S                          " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT * FROM safre_af:tabpais ",
	                " WHERE pais_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg17 FROM x_x
	      DECLARE cur_g17 CURSOR FOR curg17
	      LET pos = 1
	      FOREACH cur_g17 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PAISES ... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_colonias(xcpos_cod)
   DEFINE 
      xcpos_cod         CHAR(05), 
      aux_val		SMALLINT,
      x_x 		CHAR(300),
      x_buscar 		CHAR(30),
      pos		SMALLINT,
      reg record
         cod_colon smallint,
         colonia char(40),
         deleg   smallint, 
         ciudad  smallint,
         estado  smallint
      end record,
      desdeleg char(40),
      desciuda char(40),
      desestad char(40),
      l_reg ARRAY[1000] OF RECORD
         cod            CHAR(05),
         codigo		INTEGER,
         descripcion	CHAR(40)
      END RECORD

ERROR "BUSCANDO INFORMACION ..."

      DECLARE cur_cp CURSOR FOR
      SELECT cpos_cod,colon_cod,colon_desc 
      FROM safre_af:tab_colonia
      WHERE cpos_cod = xcpos_cod
#      ORDER BY 2 
      
      LET pos = 1
      FOREACH cur_cp INTO l_reg[pos].*
         LET pos = pos + 1
      END FOREACH
    
ERROR ""
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
	 OPEN WINDOW ventana_cp AT 6,08 WITH FORM "PRO_GLOBP3" ATTRIBUTE (BORDER)
         DISPLAY " (ENTER) Elegir                                   (Ctrl - C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY "                      C  O  L  O  N  I  A  S                       " AT 2,1 ATTRIBUTE (REVERSE,BOLD)
     
         DISPLAY ARRAY l_reg to scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

      	       SELECT deleg_cod,ciudad_cod,estad_cod 
               INTO   reg.deleg,reg.ciudad,reg.estado
               FROM safre_af:tab_codpos  
               WHERE cpos_cod = l_reg[pos].cod

               SELECT deleg_desc 
               INTO desdeleg 
               FROM safre_af:tab_delegacion 
               WHERE deleg_cod = reg.deleg

               SELECT ciudad_desc INTO desciuda
               FROM safre_af:tab_ciudad WHERE
               ciudad_cod = reg.ciudad

               SELECT estad_desc INTO desestad
               FROM safre_af:tab_estado WHERE
               estad_cod = reg.estado

               EXIT DISPLAY
            ON KEY(INTERRUPT)
               LET pos = ARR_CURR()
               LET l_reg[pos].descripcion = NULL
               LET reg.deleg = NULL
               LET desdeleg  = NULL
               LET reg.ciudad= NULL
               LET desciuda  = NULL
               LET reg.estado= NULL
               LET desestad  = NULL
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_cp
      ELSE
         ERROR "ARCHIVO DE COLONIAS ..... VACIO"
      END IF
      RETURN   
         l_reg[pos].descripcion,
         reg.deleg,
         desdeleg,
         reg.ciudad,
         desciuda,
         reg.estado,
         desestad 

END FUNCTION

FUNCTION Despliega_codigo_postal(vestad)
#ff
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
               cod              char(05),
	       descrip       	char(25),
	       descripcion	CHAR(25)
	END RECORD,
      reg record
         cod_colon smallint,
         colonia char(40),
         deleg   smallint, 
         ciudad  smallint,
         estado  smallint
      end record,
      desdeleg char(40),
      desciuda char(40),
      desestad char(40),
      vestad smallint 
	DEFINE x_x		char(300),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
        #CALL Despliega_estados()

	OPEN WINDOW vent_1 AT 05,07 WITH FORM "PRO_GLOBP4" ATTRIBUTE(BORDER)
	DISPLAY "                        CODIGOS POSTALES                          " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHENEVER ERROR CONTINUE
        PROMPT 'DIGITA CODIGO ESTADO DE ESTA COLONIA ...' for vestad attribute(reverse)
        WHENEVER ERROR STOP

        if vestad is null then
           ERROR "SOLO PUEDE SER CODIGO" SLEEP 1 
        end if
        if vestad is null then
           let vestad=0
        end if

ERROR "BUSCANDO INFORMACION ..."

	WHILE TRUE
            LET x_x = " SELECT c.cpos_cod  ,",
                      "        a.colon_desc,",
                      "        b.deleg_desc ",
                      " FROM   tab_codpos c,tab_colonia a,tab_delegacion b ",
                      " WHERE  c.cpos_cod   = a.cpos_cod ",
                      " AND    c.deleg_cod  = b.deleg_cod ",
                      " AND    a.colon_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                      " AND    c.estad_cod  =",vestad CLIPPED,
                      " ORDER BY 2 " CLIPPED

	      PREPARE curg21 FROM x_x
	      DECLARE cur_g21 CURSOR FOR curg21
	      LET pos = 1
	      FOREACH cur_g21 INTO l_reg[pos].*
                 if status=100 then
                    exit foreach
                 end if
		 LET pos = pos + 1
		 IF pos >= 1000 THEN
		    ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
       		    EXIT FOREACH
		 END IF
	      END FOREACH
ERROR ""
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO CODIGOS POSTALES... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
                         LET pos = ARR_CURR()
                         LET l_reg[pos].descripcion = NULL
                         LET reg.deleg = NULL
                         LET desdeleg  = NULL
                         LET reg.ciudad= NULL
                         LET desciuda  = NULL
                         LET reg.estado= NULL
                         LET desestad  = NULL
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
      	                 SELECT deleg_cod,ciudad_cod,estad_cod 
                         INTO   reg.deleg,reg.ciudad,reg.estado
                         FROM safre_af:tab_codpos  
                         WHERE cpos_cod = l_reg[pos].cod
          
                         SELECT deleg_desc 
                         INTO desdeleg 
                         FROM safre_af:tab_delegacion 
                         WHERE deleg_cod = reg.deleg

                         SELECT ciudad_desc INTO desciuda
                         FROM safre_af:tab_ciudad WHERE
                         ciudad_cod = reg.ciudad
          
                         SELECT estad_desc INTO desestad
                         FROM safre_af:tab_estado WHERE
                         estad_cod = reg.estado

	                 EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE

	CLOSE WINDOW vent_1
      RETURN   
         l_reg[pos].cod,
         l_reg[pos].descrip,
         reg.deleg,
         desdeleg,
         reg.ciudad,
         desciuda,
         reg.estado,
         desestad 
END FUNCTION
###################-- agrego funcion miguel#################################
FUNCTION Despliega_codigo_postal_1()
   DEFINE aux_vas SMALLINT

   DEFINE l_reg ARRAY[1000] OF RECORD
          cod           CHAR(05),
          descrip       CHAR(25),
          descripcion   CHAR(25)
   END RECORD,
          reg       RECORD
          cod_colon SMALLINT,
          colonia   CHAR(40),
          deleg     SMALLINT,
          ciudad    SMALLINT,
          estado    SMALLINT
   END RECORD,
          desdeleg  CHAR(40),
          desciuda  CHAR(40),
          desestad  CHAR(40),
          codigo_estado SMALLINT,
          vestad    SMALLINT

   DEFINE x_x      CHAR(300),
          x_buscar CHAR(30)

   DEFINE pos      SMALLINT,
          codigo   INTEGER

   OPEN WINDOW vent_xx AT 05,07 WITH FORM "PRO_GLOBP4" ATTRIBUTE(BORDER)

   DISPLAY "                        CODIGOS POSTALES                          " AT 2,1 ATTRIBUTE(REVERSE)

{
   INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT

   WHENEVER ERROR CONTINUE

   PROMPT 'DIGITA CODIGO ESTADO DE ESTA COLONIA ...' for vestad attribute(reverse)

   WHENEVER ERROR STOP

   IF vestad IS NULL THEN
       LET vestad=0
   END IF
}
   ERROR "BUSCANDO INFORMACION ..."

   WHILE TRUE


   LET x_x = " SELECT c.cpos_cod,a.colon_desc,b.deleg_desc FROM ",
             " tab_codpos c,tab_colonia a,tab_delegacion b ",
             " WHERE c.cpos_cod=a.cpos_cod and c.deleg_cod=b.deleg_cod ",
       --      " and a.colon_desc MATCHES ",'"*',x_buscar CLIPPED,'*"',
             " and a.colon_desc MATCHES '*' ",
             " and c.estad_cod=",vestad CLIPPED,
             " ORDER BY 2 " CLIPPED

   PREPARE curg21_x FROM x_x

   DECLARE cur_g21_x CURSOR FOR curg21_x
   LET pos = 1
   FOREACH cur_g21_x INTO l_reg[pos].*
      IF status=100 THEN
         EXIT FOREACH
      END IF

      LET pos = pos + 1

      IF pos >= 1000 THEN
         ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
         EXIT FOREACH
      END IF
   END FOREACH
   ERROR ""

   IF (pos-1) < 1 THEN
      ERROR "ARCHIVO CODIGOS POSTALES... VACIO"
   END IF

   CALL SET_COUNT(pos-1)

   DISPLAY ARRAY l_reg TO scr_1.*

       ON KEY ( INTERRUPT )
          LET pos = 0
          LET pos = ARR_CURR()
          LET l_reg[pos].descripcion = NULL
          LET reg.deleg = NULL
          LET desdeleg  = NULL
          LET reg.ciudad= NULL
          LET desciuda  = NULL
          LET reg.estado= NULL
          LET desestad  = NULL
          EXIT DISPLAY

       ON KEY ( CONTROL-M )
          LET codigo = l_reg[pos].cod
          LET pos = ARR_CURR()
          SELECT deleg_cod,ciudad_cod,estad_cod
          INTO   reg.deleg,reg.ciudad,reg.estado
          FROM tab_codpos
          WHERE cpos_cod = l_reg[pos].cod

          SELECT deleg_desc
          INTO desdeleg
          FROM tab_delegacion
          WHERE deleg_cod = reg.deleg

          SELECT ciudad_desc INTO desciuda
          FROM tab_ciudad
          WHERE ciudad_cod = reg.ciudad

          SELECT estad_desc INTO desestad
          FROM tab_estado
          WHERE estad_cod = reg.estado

          EXIT DISPLAY
   END DISPLAY

   IF pos <> 0 THEN
      EXIT WHILE
   END IF
   END WHILE

   CLOSE WINDOW vent_xx
   RETURN l_reg[pos].cod,       -- codigo_colonia
          l_reg[pos].descrip,   -- descripcion colonia
          reg.deleg,            -- codigo_delegacion
          desdeleg,             -- descripcion delegacion
          reg.ciudad,           -- codigo_ciudad
          desciuda,             -- descripcion ciudad
          reg.estado,           -- codigo_estado
          desestad              -- descripcion estado
END FUNCTION

FUNCTION valida_sobregiro2(v_folio,v_retiro)
#vs----------------------------------------
    DEFINE reg_7 RECORD #loc #reg_7
        nss                 CHAR(11)      ,
        subcuenta           SMALLINT      ,
        monto_en_acciones   DECIMAL(16,6)
    END RECORD

    DEFINE reg_13 RECORD #loc #reg_13
        subcuenta                    ,
        siefore             SMALLINT ,
        monto_acc                    ,
        monto_pesos         DECIMAL(16,6)
    END RECORD

    DEFINE  #loc #char
        v_retiro            CHAR(001) ,
        v_saldo_dia         CHAR(150) ,
        v_diag_registro     CHAR(003)

    DEFINE #loc #smallint
        v_grupo             ,
        v_subcuenta         SMALLINT

    DEFINE #loc #date
        v_fecha             DATE

    DEFINE #loc #integer
        sw_1                ,
        v_folio             INTEGER

    DEFINE #loc #decimal
        monto_accion_97     ,
        monto_accion_cv     ,
        monto_accion_es     ,
        monto_accion_esp    ,
        monto_accion_so     ,
        monto_accion_ret92  ,
        monto_particip_v97  ,
        monto_particip_v92  DECIMAL(16,6)

    LET sw_1                    = 0
    LET v_fecha                 = HOY
    LET reg_7.monto_en_acciones = 0
    LET v_grupo                 = 0
    LET monto_accion_97         = 0
    LET monto_accion_cv         = 0
    LET monto_accion_es         = 0
    LET monto_accion_esp        = 0
    LET monto_accion_so         = 0
    LET monto_accion_ret92      = 0
    LET monto_particip_v97      = 0
    LET monto_particip_v92      = 0


    DECLARE  cur_4 CURSOR FOR
    SELECT  A.nss                    ,
            A.subcuenta              ,
            SUM(A.monto_en_acciones) * -1 ,
            B.diag_registro          
    FROM    dis_provision A, ret_solicitud_rx B
    WHERE   A.folio            = v_folio
    AND     B.tipo_retiro      = v_retiro
    AND     A.nss              = B.nss
    AND     A.consecutivo_lote = B.consecutivo
    AND     B.diag_registro    IN (400,440)
    GROUP   BY 1,2,4

    FOREACH cur_4 INTO reg_7.*,v_diag_registro
        LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"

        PREPARE eje_saldo_dia FROM v_saldo_dia

        DECLARE c_saldo  CURSOR FOR eje_saldo_dia
        FOREACH c_saldo  USING reg_7.nss       ,
                               reg_7.subcuenta ,
                               v_grupo         ,
                               v_fecha
                         INTO  reg_13.subcuenta   ,
                               reg_13.siefore     ,
                               reg_13.monto_acc   ,
                               reg_13.monto_pesos

            CASE reg_7.subcuenta
                WHEN 1
                    LET monto_accion_97    = monto_accion_97  +
                                             reg_13.monto_acc
                WHEN 2
                    LET monto_accion_cv    = monto_accion_cv +
                                             reg_13.monto_acc
                WHEN 4
                    LET monto_particip_v97 = monto_particip_v97 +
                                             reg_13.monto_acc
                WHEN 5
                    LET monto_accion_so    = monto_accion_so +
                                             reg_13.monto_acc
                WHEN 6
                    LET monto_accion_es    = monto_accion_es +
                                             reg_13.monto_acc
                WHEN 7
                    LET monto_accion_ret92 = monto_accion_ret92 +
                                             reg_13.monto_acc
                WHEN 8
                    LET monto_particip_v92 = monto_particip_v92 +
                                             reg_13.monto_acc
                WHEN 9
                    LET monto_accion_esp   = monto_accion_esp +
                                             reg_13.monto_acc
            END CASE
        END FOREACH

        CASE reg_7.subcuenta
            WHEN 1
                IF reg_7.monto_en_acciones > monto_accion_97 THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
            WHEN 2
                IF reg_7.monto_en_acciones > monto_accion_cv THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
            WHEN 4
                IF v_diag_registro = 400 THEN
                    IF reg_7.monto_en_acciones > monto_particip_v97 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
                ELSE
                    LET sw_1 = 0
                END IF 
            WHEN 5
                IF reg_7.monto_en_acciones > monto_accion_so THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
            WHEN 6
                IF reg_7.monto_en_acciones > monto_accion_es THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
            WHEN 7
                IF reg_7.monto_en_acciones > monto_accion_ret92 THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
            WHEN 8
                IF v_diag_registro = 400 THEN 
                    IF reg_7.monto_en_acciones > monto_particip_v92 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
                ELSE
                    LET sw_1 = 0
                END IF
            WHEN 9
                IF reg_7.monto_en_acciones > monto_accion_esp THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
        END CASE
    END FOREACH

    RETURN sw_1,reg_7.nss
END FUNCTION

###############################################################################
#Proyecto          => SAFRE  ( MEXICO )                                       #
#Owner             => E.F.P  					              # 
#Programa DISB010I => INTEGRA CALCULO DE cta_cms_saldo_ind EN dis_cuenta           #
#                  => opc = 1 INTEGRACION DIRECTA,  opc = 2 POR ARCHIVO       #
#Creado            => 24 Julio  1998.    				      #
#By                => HECTOR M. FERNANDEZ A.              		      #
#Actualizacion     => 26 JUNIO 2001      				      #
#Sistema           => DIS   					              #
###############################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_cta   RECORD LIKE dis_cuenta.*

    DEFINE tot_com RECORD
               nss		CHAR(11),
               subcuenta        SMALLINT,
	       fecha_aplica	DATE,
	       total_comision	DECIMAL(20,6),
	       total_acciones	DECIMAL(20,6),
	       total_pesos	DECIMAL(20,6)
    END RECORD

    DEFINE
        x_siefore		SMALLINT,
        x_folio			INTEGER,
        x_id_aportante		CHAR(20),
        hist_fecha_aplica	DATE,
        HOY			DATE,
        opc			CHAR(1),
        usuario			CHAR(8)
 
END GLOBALS

###############################################################################
MAIN
    OPTIONS PROMPT LINE last,
    ACCEPT KEY CONTROL-I

    CALL init() #i
    CALL crea_cuentas() #cc
    #CALL actualiza_historico() #ah
    #CALL borra_anterior() #ba

END MAIN

###############################################################################
FUNCTION init()
#i ------------

    SELECT USER
    INTO   usuario
    FROM   glo_parametro

    LET HOY = TODAY
    LET opc = 1

    #AFORE XXI
    {
    LET x_siefore      = 1	#516
    LET x_folio        = 5162000
    LET x_id_aportante = "XXI"
    }

    #AFORE TEPEYAC
    LET x_siefore      = 1	#514
    LET x_folio        = 5142000
    LET x_id_aportante = "TEPEYAC"

END FUNCTION

###############################################################################
FUNCTION crea_cuentas()
#cc--------------------

    LET g_cta.tipo_movimiento	= 110
    LET g_cta.siefore		= x_siefore
    LET g_cta.folio		= x_folio
    LET g_cta.curp		= ""
    LET g_cta.folio_sua		= 0
    LET g_cta.monto_en_acciones	= 0
    LET g_cta.precio_accion	= 1
    LET g_cta.dias_cotizados	= 0
    LET g_cta.sucursal		= ""
    LET g_cta.id_aportante	= x_id_aportante
    LET g_cta.estado		= 5
    LET g_cta.fecha_proceso	= HOY
    LET g_cta.usuario		= usuario
    LET g_cta.fecha_archivo	= ""
    LET g_cta.etiqueta		= 1

    DECLARE cursor_com CURSOR FOR
       SELECT nss, 
              subcuenta,
              fecha_aplica,
              SUM(monto_provision),
	      SUM(total_acciones)
       FROM   cta_cms_saldo_ind
       GROUP  BY 1,2,3
       ORDER  BY 1,2,3

    IF opc= 2 THEN
        START REPORT listado TO "/home/safre/FTE/DIS/comissal.unl"
    END IF

    FOREACH cursor_com INTO tot_com.*
	IF opc = 1 THEN
	    CALL inserta_cuentas(tot_com.*) #ic
	ELSE
            OUTPUT TO REPORT listado(tot_com.*) #l
	END IF
	#CALL actualiza_saldo(tot_com.*) #as
    END FOREACH

    IF opc= 2 THEN
        FINISH REPORT listado
    END IF

END FUNCTION

############################################################################
FUNCTION inserta_cuentas(tot_com)
#ic------------------------------

    DEFINE tot_com RECORD
        nss		CHAR(11),
        subcuenta       SMALLINT,
        fecha_aplica	DATE,
        total_comision	DECIMAL(20,6),
        total_acciones	DECIMAL(20,6),
	total_pesos	DECIMAL(20,6)
    END RECORD
 
    LET g_cta.subcuenta		= tot_com.subcuenta
    LET g_cta.nss		= tot_com.nss
    LET g_cta.monto_en_pesos	= tot_com.total_comision * (-1)
    LET g_cta.fecha_pago	= tot_com.fecha_aplica
    LET g_cta.fecha_valor	= tot_com.fecha_aplica - 1
    LET g_cta.fecha_conversion	= tot_com.fecha_aplica

    INSERT INTO cuentas_saldos VALUES (g_cta.*)

END FUNCTION

############################################################################
FUNCTION actualiza_saldo(tot_com)
#as------------------------------

    DEFINE tot_com RECORD
        nss		CHAR(11),
        subcuenta       SMALLINT,
        fecha_aplica	DATE,
        total_comision	DECIMAL(20,6),
        total_acciones	DECIMAL(20,6),
        total_pesos   	DECIMAL(20,6)
    END RECORD

    DEFINE
	x_precio_dia	DECIMAL(20,6)


    SELECT precio_del_dia
    INTO   x_precio_dia
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = HOY

    IF x_precio_dia IS NULL THEN
	LET x_precio_dia = 1
    END IF

    LET tot_com.total_pesos = tot_com.total_acciones * x_precio_dia

    SELECT "X"
    FROM   saldo_cuentas_h
    WHERE  nss       = tot_com.nss
    AND    subcuenta = tot_com.subcuenta

    IF STATUS = NOTFOUND THEN
	INSERT INTO saldo_cuentas_h
		    VALUES (tot_com.nss,
                            tot_com.subcuenta,
                            1,
                            tot_com.fecha_aplica,
                            tot_com.fecha_aplica,
                            tot_com.total_pesos,
                            tot_com.total_acciones,
                            USUARIO
                           )
    ELSE
        UPDATE saldo_cuentas
        SET    monto_en_acciones = tot_com.total_acciones,
               monto_en_pesos    = tot_com.total_pesos,
	       fecha_valor       = tot_com.fecha_aplica,
	       fecha_conversion  = tot_com.fecha_aplica
        WHERE  nss       = tot_com.nss
        AND    subcuenta = tot_com.subcuenta

    END IF

END FUNCTION

############################################################################
REPORT listado(tot_com)
#l---------------------

    DEFINE tot_com RECORD
        nss		CHAR(11),
        subcuenta        SMALLINT,
        fecha_aplica	DATE,
        total_comision	DECIMAL(20,6),
        total_acciones	DECIMAL(20,6),
        total_pesos   	DECIMAL(20,6)
    END RECORD

    OUTPUT 
       PAGE LENGTH 1
       LEFT MARGIN 0
       RIGHT MARGIN 0
       TOP MARGIN 0
       BOTTOM MARGIN 0

    FORMAT
       ON EVERY ROW
	  IF tot_com.total_comision IS NULL THEN
             LET tot_com.total_comision = 0
	  END IF
 
          LET g_cta.subcuenta		= tot_com.subcuenta
    	  LET g_cta.nss			= tot_com.nss
    	  LET g_cta.monto_en_pesos	= tot_com.total_comision * (-1)
    	  LET g_cta.fecha_pago		= tot_com.fecha_aplica
    	  LET g_cta.fecha_valor		= tot_com.fecha_aplica - 1
    	  LET g_cta.fecha_conversion	= tot_com.fecha_aplica
 
          PRINT 
             g_cta.tipo_movimiento,"|",
             g_cta.subcuenta,"|",
             g_cta.siefore,"|",
             g_cta.folio,"|",
             g_cta.nss,"|",
             g_cta.curp CLIPPED,"|",
             g_cta.folio_sua CLIPPED,"|",
             g_cta.fecha_pago,"|",
             g_cta.fecha_valor,"|",
             g_cta.fecha_conversion,"|",
             g_cta.monto_en_pesos,"|",
             g_cta.monto_en_acciones,"|",
             g_cta.precio_accion,"|",
             g_cta.dias_cotizados,"|",
             g_cta.sucursal CLIPPED,"|",
             g_cta.id_aportante CLIPPED,"|",
             g_cta.estado,"|",
             g_cta.fecha_proceso,"|",
             g_cta.usuario CLIPPED,"|",
             g_cta.fecha_archivo CLIPPED,"|",
             g_cta.etiqueta,"|"
END REPORT
 
############################################################################
FUNCTION actualiza_historico()
#ah---------------------------

    DEFINE
	x_subcuenta		SMALLINT,
	x_ctas_proc		INTEGER

    ERROR "ACTUALIZANDO HISTORIA"

    DECLARE cur_sal CURSOR FOR
        SELECT subcuenta, COUNT(DISTINCT nss)
        FROM   cta_cms_saldo_ind
	GROUP  BY 1

    FOREACH cur_sal INTO x_subcuenta, x_ctas_proc

        UPDATE hist_comis_sal
        SET    ctas_proc   = x_ctas_proc,
	       estado      = 2
        WHERE  subcuenta   = x_subcuenta
        AND    estado      = 1
 
    END FOREACH

    UPDATE hist_comis_sal
    SET    estado = 2
    WHERE  estado = 1

END FUNCTION

############################################################################
FUNCTION borra_anterior()
#ba----------------------

    DEFINE
	x_fecha_aplica	DATE,
	x_fecha_max	DATE,
	x_estado	SMALLINT


    SELECT MAX(fecha_aplica)
    INTO   x_fecha_max
    FROM   cta_cms_saldo_ind

    SELECT MAX(fecha_aplica), MIN(estado)
    INTO   x_fecha_aplica, x_estado
    FROM   hist_comis_sal

    IF x_estado = 2 AND x_fecha_aplica = x_fecha_max THEN
        ERROR "BORRANDO CALCULOS DE comis_saldo_ind"
        DROP TABLE cta_cms_saldo_ind

        CREATE TABLE "informix".comis_saldo_ind 
          (
            nss char(11) not null ,
            subcuenta smallint not null ,
            fecha_aplica date not null ,
            monto_provision decimal(16,6) not null ,
            total_acciones decimal(16,6)
          );
        
        CREATE INDEX "informix".comis_saldo_ind1 ON 
		     "informix".comis_saldo_ind (nss,subcuenta,fecha_aplica);

    ELSE
	ERROR "INCONSISTENCIA DE DATOS, cta_cms_saldo_ind NO BORRADO"
    END IF

END FUNCTION
